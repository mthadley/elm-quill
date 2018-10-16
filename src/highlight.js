import Quill from 'quill';

const Inline = Quill.import('blots/inline');

export default class Highlight extends Inline {
	static blotName = 'highlight';
	static className = 'highlight';
	static tagName = 'strong';

	static create(value) {
		const node = super.create(value);

		node.setAttribute('role', 'button');
		node.setAttribute('tabindex', 0);

		return node;
	}

	constructor(...args) {
		super(...args);

		if (this.domNode) {
			this.domNode.addEventListener('click', this.handleClick);
		}
	}

	/**
	 * TODO: Move this logic to the Elm side, possibly by triggering
	 * a custom event here.
	 */
	handleClick = (event) => {
		event.stopPropagation();

		const node = this.domNode.closest('#quillRoot');
		const quill = node && node.__quill;

		if (!quill) return;

		const index = this.offset(this.scroll);
		const length = this.length();

		quill.formatText(index, length, 'highlight', false);
	};
}

Quill.register(Highlight);
