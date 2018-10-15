import Quill from 'quill';

const Delta = Quill.import('delta');

/**
 * Adapts the `formats` option to be valid for use with the
 * toolbar module options
 */
function normalizeToolbarFormats(formats) {
	return formats.flatMap(
		format =>
			format === 'list' ? [{list: 'ordered'}, {list: 'bullet'}] : format,
	);
}

class ElmQuill extends HTMLElement {
	_placeholder = '';
	_readOnly = false;

	set formats(value) {
		this._formats = value;
	}

	set readOnly(value) {
		this._readOnly = value;

		if (this._quill) {
			this._quill.enable(!value);
		}
	}

	set theme(value) {
		this._theme = value;
	}

	set placeholder(value) {
		this._placeholder = value;
	}

	set content(content) {
		this._content = new Delta(content);

		this._updateQuill();
	}

	set selection(range) {
		this._selection = range;

		this._updateQuill();
	}

	connectedCallback() {
		this._initElement();
		this._initQuill();
		this._updateQuill();
	}

	_initElement() {
		this._element = document.createElement('div');
		this._element.id = 'quillRoot';

		this.appendChild(this._element);
	}

	_initQuill() {
		this._quill = new Quill(this._element, {
			formats: this._formats,
			theme: this._theme,
			placeholder: this._placeholder,
			modules: {
				toolbar: normalizeToolbarFormats(this._formats),
			},
		});

		this._quill.on('editor-change', this._handleChange);
	}

	_updateQuill() {
		if (!this._quill) return;

		if (
			this._quill
				.getContents()
				.diff(this._content)
				.length()
		) {
			this._quill.setContents(this._content, 'silent');
		}

		this._quill.setSelection(this._selection, 'silent');
	}

	_handleChange = (eventName, rangeOrDelta, oldRangeOrDelta, source) => {
		if (source === 'silent') return;

		/**
		 * Schedule the event for later, which batches any other updates.
		 */
		setTimeout(() => {
			const event = new CustomEvent('change', {
				detail: {
					delta: this._quill.getContents(),
					range: this._quill.getSelection(),
				},
			});

			this.dispatchEvent(event);
		});
	};
}

customElements.define('elm-quill', ElmQuill);
