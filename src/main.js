import './quill';
import Quill from 'quill';
import {Elm} from './Main.elm';

/* Declare our custom Highlight blot */

const Inline = Quill.import('blots/inline');

class Highlight extends Inline {
	static blotName = 'highlight';
	static className = 'highlight';
	static tagName = 'strong';

	static create(value) {
		const node = super.create(value);

		node.setAttribute('role', 'button');
		node.setAttribute('tabindex', 0);

		return node;
	}
}

Quill.register(Highlight);

/* App initializaiton */

Elm.Main.init({
	node: document.getElementById('app'),
});
