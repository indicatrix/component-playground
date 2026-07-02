// <cp-ai-selection> — the DOM half of the AI Inspector selection engine.
//
// Reference companion to Component.Application.AiInspector (Elm). A host app
// (e.g. sage) installs the same custom element next to its other playground
// custom elements. The Elm side renders <cp-ai-selection> around the live
// preview and toggles the `active` attribute in selection mode; this element:
//
//   • while active, highlights the hovered inspectable element (dashed box) and,
//     on click, captures structured metadata and dispatches a `cp-select`
//     CustomEvent whose `detail` matches AiInspector.selectedDecoder;
//   • dispatches `cp-cancel` on Escape;
//   • keeps a persistent solid outline on the committed element for as long as
//     Elm sets the `selected-path` attribute (cleared when it is removed —
//     i.e. on clear-selection, start-new-selection, or navigation); the outline
//     tracks scroll/resize and re-queries the path so it survives re-renders;
//   • is transparent to layout (the Elm side sets display:contents).
//
// It never edits the DOM it inspects — it only reads and draws overlays.

// The strong brand purple used for the inspect outline + label chip
// (brand-purple-500 — matches the design-system prototype / Theme.tokenIcon).
const OUTLINE_COLOR = "#7130FF";

const TOKEN_ATTR_PREFIX = "data-token-";

const KNOWN_CATEGORIES = new Set([
  "typography",
  "text-colour",
  "background-colour",
  "font-family",
  "line-height",
  "letter-spacing",
  "spacing",
  "radius",
  "elevation",
  "border",
  "motion",
]);

// A few computed styles worth surfacing in the inspector.
const COMPUTED_KEYS = [
  "font-size",
  "font-weight",
  "line-height",
  "letter-spacing",
  "color",
  "background-color",
  "border-radius",
];

function humanType(el) {
  const explicit = el.getAttribute("data-element");
  if (explicit) return explicit;

  const role = (el.getAttribute("role") || "").toLowerCase();
  const tag = el.tagName.toLowerCase();

  if (/^h[1-6]$/.test(tag) || role === "heading") return "Heading";
  if (tag === "button" || role === "button") return "Button";
  if (tag === "input" || tag === "textarea" || tag === "select") return "Input";
  if (tag === "a" || role === "link") return "Link";
  if (tag === "img" || role === "img") return "Image";
  if (tag === "label") return "Label";
  if (tag === "p") return "Paragraph";
  return tag.charAt(0).toUpperCase() + tag.slice(1);
}

// Human-readable label for the overlay chip, e.g. "Heading (h1)".
function labelFor(el) {
  return `${humanType(el)} (${el.tagName.toLowerCase()})`;
}

function prettifyCategory(category) {
  return category
    .split("-")
    .map((w) => w.charAt(0).toUpperCase() + w.slice(1))
    .join(" ");
}

// Walk ancestors (including el) to find the first with a given attribute.
function closestAttr(el, attr, root) {
  let node = el;
  while (node && node !== root.parentNode) {
    if (node.getAttribute && node.getAttribute(attr) != null) {
      return node.getAttribute(attr);
    }
    node = node.parentNode;
  }
  return null;
}

function collectTokens(el) {
  const tokens = [];
  for (const attr of el.attributes) {
    if (!attr.name.startsWith(TOKEN_ATTR_PREFIX)) continue;
    const category = attr.name.slice(TOKEN_ATTR_PREFIX.length);
    if (!KNOWN_CATEGORIES.has(category)) continue;
    tokens.push({
      category,
      label: prettifyCategory(category),
      value: attr.value,
      source: "class",
    });
  }
  return tokens;
}

function collectDataAttributes(el) {
  const out = {};
  for (const attr of el.attributes) {
    if (attr.name.startsWith("data-")) out[attr.name] = attr.value;
  }
  return out;
}

function collectComputed(el) {
  const cs = window.getComputedStyle(el);
  const out = {};
  for (const key of COMPUTED_KEYS) out[key] = cs.getPropertyValue(key).trim();
  return out;
}

// A reasonably stable selector path: prefer id, else tag + nth-of-type chain.
// Stops at (and excludes) the <cp-ai-selection> wrapper so the path is in terms
// of the previewed markup, not our host element.
function selectorFor(el, root) {
  if (el.id) return `#${el.id}`;
  const parts = [];
  let node = el;
  while (node && node !== root && node.nodeType === 1) {
    let part = node.tagName.toLowerCase();
    const parent = node.parentNode;
    if (parent) {
      const sameTag = Array.from(parent.children).filter(
        (c) => c.tagName === node.tagName
      );
      if (sameTag.length > 1) {
        part += `:nth-of-type(${sameTag.indexOf(node) + 1})`;
      }
    }
    parts.unshift(part);
    if (node.id) {
      parts[0] = `#${node.id}`;
      break;
    }
    node = node.parentNode;
  }
  return parts.join(" > ");
}

function currentRoute() {
  const params = new URLSearchParams(window.location.search);
  return params.get("component") || window.location.pathname;
}

function buildMetadata(el, root) {
  const tag = el.tagName.toLowerCase();
  const text = (el.textContent || "").trim().replace(/\s+/g, " ").slice(0, 140);
  const type = humanType(el);
  const componentName = closestAttr(el, "data-component", root);

  return {
    id: el.id || selectorFor(el, root),
    label: `${type} (${tag})`,
    subtitle: text || null,
    elementType: type,
    tagName: tag,
    role: el.getAttribute("role"),
    textContent: text || null,
    componentName: componentName || "",
    componentId: componentName,
    route: currentRoute(),
    sourceFile: closestAttr(el, "data-source-file", root),
    sourceSymbol: closestAttr(el, "data-source-symbol", root),
    selector: selectorFor(el, root),
    classNames: Array.from(el.classList),
    dataAttributes: collectDataAttributes(el),
    tokens: collectTokens(el),
    computedStyles: collectComputed(el),
    bounds: (() => {
      const r = el.getBoundingClientRect();
      return { x: r.x, y: r.y, width: r.width, height: r.height };
    })(),
  };
}

class CpAiSelection extends HTMLElement {
  static get observedAttributes() {
    // `active`       — selection mode on: hover highlight + click capture.
    // `selected-path`— CSS path of the committed element: keep a persistent
    //                  outline on it (cleared when the attribute is removed).
    return ["active", "selected-path"];
  }

  constructor() {
    super();
    this._hoverBox = null;
    this._selectedBox = null;
    this._selectedPath = null;
    this._onMove = this._onMove.bind(this);
    this._onClick = this._onClick.bind(this);
    this._onKey = this._onKey.bind(this);
    this._reposition = this._reposition.bind(this);
    this._hovered = null;
  }

  connectedCallback() {
    // Keep the persistent selected outline aligned as the page scrolls/resizes.
    window.addEventListener("scroll", this._reposition, true);
    window.addEventListener("resize", this._reposition);
    if (this.hasAttribute("active")) this._enable();
    this._syncSelected();
  }

  attributeChangedCallback(name) {
    if (name === "active") {
      if (this.hasAttribute("active")) this._enable();
      else this._disable();
    } else if (name === "selected-path") {
      this._syncSelected();
    }
  }

  disconnectedCallback() {
    this._disable();
    window.removeEventListener("scroll", this._reposition, true);
    window.removeEventListener("resize", this._reposition);
    this._clearSelected();
  }

  // ---- selection mode (transient hover) ----

  _enable() {
    this._ensureHoverBox();
    // Capture phase so we intercept clicks before the previewed component.
    this.addEventListener("pointermove", this._onMove, true);
    this.addEventListener("click", this._onClick, true);
    document.addEventListener("keydown", this._onKey, true);
    document.body.style.cursor = "crosshair";
  }

  _disable() {
    this.removeEventListener("pointermove", this._onMove, true);
    this.removeEventListener("click", this._onClick, true);
    document.removeEventListener("keydown", this._onKey, true);
    document.body.style.cursor = "";
    // Remove the transient hover box entirely (not just hide) so instances
    // don't leave orphaned nodes in <body>. The selected outline is separate.
    if (this._hoverBox) {
      this._hoverBox.remove();
      this._hoverBox = null;
    }
    this._hovered = null;
  }

  // The inspectable target for a raw event target: the nearest ancestor marked
  // [data-ai-inspectable], else the raw element (still scoped within us).
  _target(node) {
    if (!node || !this.contains(node)) return null;
    const marked = node.closest("[data-ai-inspectable]");
    if (marked && this.contains(marked)) return marked;
    return node.nodeType === 1 ? node : node.parentElement;
  }

  _onMove(event) {
    const target = this._target(event.target);
    if (!target) {
      this._hideHoverBox();
      this._hovered = null;
      return;
    }
    this._hovered = target;
    this._setChip(this._hoverBox, labelFor(target), false);
    this._positionBox(this._hoverBox, target);
  }

  _onClick(event) {
    const target = this._target(event.target);
    if (!target) return;
    event.preventDefault();
    event.stopPropagation();
    const detail = buildMetadata(target, this);
    this.dispatchEvent(
      new CustomEvent("cp-select", { detail, bubbles: true, composed: true })
    );
    // Elm leaves selection mode on capture; drop the active attribute so we go
    // inert even before the next render lands. The persistent selected outline
    // is driven by the `selected-path` attribute Elm sets in response.
    this.removeAttribute("active");
  }

  _onKey(event) {
    if (event.key === "Escape") {
      event.preventDefault();
      this.dispatchEvent(
        new CustomEvent("cp-cancel", { bubbles: true, composed: true })
      );
      this.removeAttribute("active");
    }
  }

  // ---- persistent selected outline ----

  _syncSelected() {
    const path = this.getAttribute("selected-path");
    if (path) {
      this._selectedPath = path;
      this._drawSelected();
    } else {
      this._clearSelected();
    }
  }

  _selectedEl() {
    if (!this._selectedPath) return null;
    try {
      return this.querySelector(this._selectedPath);
    } catch (_) {
      return null;
    }
  }

  _drawSelected() {
    const el = this._selectedEl();
    if (!el) {
      if (this._selectedBox) this._selectedBox.style.display = "none";
      return;
    }
    this._ensureSelectedBox();
    this._setChip(this._selectedBox, labelFor(el), true);
    this._positionBox(this._selectedBox, el);
  }

  _clearSelected() {
    this._selectedPath = null;
    if (this._selectedBox) {
      this._selectedBox.remove();
      this._selectedBox = null;
    }
  }

  _reposition() {
    // Cheap: only the persistent selected box needs to track scroll/resize.
    if (this._selectedPath) this._drawSelected();
  }

  // ---- overlay boxes + label chips ----
  //
  // Each overlay is a fixed-position outline box (purple) with a purple label
  // chip anchored at its top-left. Hover = dashed, no close. Selected = solid +
  // glow, with a × inside the chip that dispatches `cp-clear` (the only preview
  // control for clearing the selection). Boxes are pointer-events:none so they
  // don't block the element; only the selected chip's × opts back in.

  _ensureHoverBox() {
    if (this._hoverBox) return;
    this._hoverBox = this._makeBox("hover");
  }

  _ensureSelectedBox() {
    if (this._selectedBox) return;
    this._selectedBox = this._makeBox("selected");
  }

  _makeBox(kind) {
    const selected = kind === "selected";
    const box = document.createElement("div");
    box.style.cssText = [
      "position:fixed",
      "pointer-events:none",
      "z-index:2147483646",
      "border-radius:3px",
      "transition:all 60ms ease-out",
      "display:none",
      selected ? `border:2px solid ${OUTLINE_COLOR}` : `border:2px dashed ${OUTLINE_COLOR}`,
      // Selected: outline + glow only, no fill. Hover keeps a faint tint so the
      // hovered target reads while scanning.
      selected ? "" : "background:rgba(113,48,255,0.06)",
      selected
        ? "box-shadow:0 0 0 1px rgba(113,48,255,0.35), 0 0 0 4px rgba(113,48,255,0.15)"
        : "",
    ]
      .filter(Boolean)
      .join(";");

    // Label chip anchored just above the top-left corner.
    const chip = document.createElement("div");
    chip.style.cssText = [
      "position:absolute",
      "top:0",
      "left:0",
      "transform:translateY(-100%)",
      "margin-top:-2px",
      "display:flex",
      "align-items:center",
      "gap:6px",
      `background:${OUTLINE_COLOR}`,
      "color:#ffffff",
      "font:600 11px/1.4 Inter, system-ui, sans-serif",
      "padding:2px 6px",
      "border-radius:4px",
      "white-space:nowrap",
      "box-shadow:0 1px 2px rgba(16,24,40,0.2)",
      // Selected chip must be clickable (its ×); hover chip stays inert.
      selected ? "pointer-events:auto" : "pointer-events:none",
    ].join(";");
    box._chip = chip;
    box._chipLabel = null;
    box.appendChild(chip);

    document.body.appendChild(box);
    return box;
  }

  _setChip(box, label, withClose) {
    if (!box) return;
    const chip = box._chip;
    // Avoid rebuilding the DOM every pointermove — only update on change.
    if (box._chipLabel === label && box._chipClose === withClose) return;
    box._chipLabel = label;
    box._chipClose = withClose;
    chip.textContent = "";
    const text = document.createElement("span");
    text.textContent = label;
    chip.appendChild(text);
    if (withClose) {
      const x = document.createElement("span");
      x.textContent = "×";
      x.setAttribute("role", "button");
      x.setAttribute("aria-label", "Clear selection");
      x.style.cssText = [
        "cursor:pointer",
        "pointer-events:auto",
        "font-size:14px",
        "line-height:1",
        "padding:0 1px",
        "opacity:0.9",
      ].join(";");
      x.addEventListener("click", (e) => {
        e.preventDefault();
        e.stopPropagation();
        // Dispatched on the host element so Elm's listener on <cp-ai-selection>
        // receives it and clears the selection.
        this.dispatchEvent(
          new CustomEvent("cp-clear", { bubbles: true, composed: true })
        );
      });
      chip.appendChild(x);
    }
  }

  _positionBox(box, el) {
    if (!box) return;
    const r = el.getBoundingClientRect();
    const s = box.style;
    s.display = "block";
    s.left = `${r.left}px`;
    s.top = `${r.top}px`;
    s.width = `${r.width}px`;
    s.height = `${r.height}px`;
  }

  _hideHoverBox() {
    if (this._hoverBox) this._hoverBox.style.display = "none";
  }
}

export function registerCpAiSelection() {
  if (!customElements.get("cp-ai-selection")) {
    customElements.define("cp-ai-selection", CpAiSelection);
  }
}

registerCpAiSelection();
