export namespace Elm {
	namespace Main {
		interface App {
			ports: Ports;
		}
		
		interface Args {
			node: HTMLElement;
		}
		
		interface Ports {
			requestSearch: Subscribe<string>;
			searchReceiver: Send<string[]>;
		}
		
		function init(args: Args): App;

		interface Subscribe<T> {
			subscribe(callback: (value: T) => void): void;
		}

		interface Send<T> {
			send(value: T): void;
		}
	}	
}