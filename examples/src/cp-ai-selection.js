// <cp-ai-selection> — the DOM half of the AI Inspector selection engine.
//
// Reference companion to Component.Application.AiInspector (Elm). A host app
// (e.g. sage) installs the same custom element next to its other playground
// custom elements. The Elm side renders <cp-ai-selection> around the live
// preview and toggles the `active` attribute in selection mode; this element:
//
//   • while active, highlights the hovered inspectable element and, on click,
//     captures structured metadata and dispatches a `cp-select` CustomEvent
//     whose `detail` matches AiInspector.selectedDecoder;
//   • dispatches `cp-cancel` on Escape;
//   • is transparent to layout (the Elm side sets display:contents) and does
//     nothing at all when inactive.
//
// It never edits the DOM it inspects — it only reads and draws an overlay.

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
    return ["active"];
  }

  constructor() {
    super();
    this._overlay = null;
    this._onMove = this._onMove.bind(this);
    this._onClick = this._onClick.bind(this);
    this._onKey = this._onKey.bind(this);
    this._hovered = null;
  }

  attributeChangedCallback(name) {
    if (name === "active") {
      if (this.hasAttribute("active")) this._enable();
      else this._disable();
    }
  }

  disconnectedCallback() {
    this._disable();
  }

  _enable() {
    this._ensureOverlay();
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
    // Remove the overlay entirely (not just hide) so instances don't leave
    // orphaned nodes in <body> as the user navigates between components.
    if (this._overlay) {
      this._overlay.remove();
      this._overlay = null;
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
      this._hideOverlay();
      this._hovered = null;
      return;
    }
    this._hovered = target;
    this._positionOverlay(target);
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
    // inert even before the next render lands.
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

  _ensureOverlay() {
    if (this._overlay) return;
    const box = document.createElement("div");
    box.style.cssText = [
      "position:fixed",
      "pointer-events:none",
      "z-index:2147483646",
      "border:2px solid #2F7FFE",
      "background:rgba(47,127,254,0.12)",
      "border-radius:3px",
      "transition:all 60ms ease-out",
      "display:none",
    ].join(";");
    document.body.appendChild(box);
    this._overlay = box;
  }

  _positionOverlay(el) {
    if (!this._overlay) return;
    const r = el.getBoundingClientRect();
    const s = this._overlay.style;
    s.display = "block";
    s.left = `${r.left}px`;
    s.top = `${r.top}px`;
    s.width = `${r.width}px`;
    s.height = `${r.height}px`;
  }

  _hideOverlay() {
    if (this._overlay) this._overlay.style.display = "none";
  }
}

export function registerCpAiSelection() {
  if (!customElements.get("cp-ai-selection")) {
    customElements.define("cp-ai-selection", CpAiSelection);
  }
}

registerCpAiSelection();
