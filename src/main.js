import './quill';
import './highlight';
import Quill from 'quill';
import {Elm} from './Main.elm';

Elm.Main.init({
	node: document.getElementById('app'),
});
