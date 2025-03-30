export namespace Elm {
    // Might be able to tweak this a bit!
    // Eg: generics for init function, use a var for Index.
    namespace Index {
        export interface App {
            ports: {
                hello: {
                    subscribe(callback: (data: string) => void): void
                }
                reply: {
                    send(data: number): void
                }
            };
        }
        export function init(options: {
            node?: HTMLElement | null;
            flags: null;
        }): Elm.Index.App;
    }
}