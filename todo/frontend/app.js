// Mock for view
const signedIn = false

const showSignInView = () => {
	const app = document.getElementById('app')

	// REGISTER
	const register = addElement(app, 'h2', 'register', [], 'Register')

	const registerDiv = addElement(app, 'div', 'registerDiv')

	const emailDiv1 = addElement(registerDiv, 'div', 'emailDiv1')

	const emailLabel1 = addElement(
		emailDiv1,
		'label',
		'emailLabel1',
		[],
		'Email'
	)

	const emailField1 = addElement(emailDiv1, 'input', 'emailField1', [
		['type', 'email'],
	])

	const passwordDiv1 = addElement(registerDiv, 'div', 'passwordDiv1')

	const passwordLabel1 = addElement(
		passwordDiv1,
		'label',
		'passwordLabel1',
		[],
		'Password'
	)

	const passwordField1 = addElement(passwordDiv1, 'input', 'passwordField1', [
		['type', 'password'],
	])

	const submitButton1 = addElement(
		registerDiv,
		'button',
		'submitButton1',
		[],
		'Submit'
	)

	// SIGN IN
	const signin = addElement(app, 'h2', 'signin', [], 'Sign In')

	const signinDiv = addElement(app, 'div', 'signinDiv')

	const emailDiv2 = addElement(signinDiv, 'div', 'emailDiv2')

	const emailLabel2 = addElement(
		emailDiv2,
		'label',
		'emailLabel2',
		[],
		'Email'
	)

	const emailField2 = addElement(emailDiv2, 'input', 'emailField2', [
		['type', 'email'],
	])

	const passwordDiv2 = addElement(signinDiv, 'div', 'passwordDiv2')

	const passwordLabel2 = addElement(
		passwordDiv2,
		'label',
		'passwordLabel2',
		[],
		'Password'
	)

	const passwordField2 = addElement(passwordDiv2, 'input', 'passwordField2', [
		['type', 'password'],
	])

	const submitButton2 = addElement(
		signinDiv,
		'button',
		'submitButton2',
		[],
		'Submit'
	)
}

const showListView = () => {
	const app = document.getElementById('app')

	// Top Bar
	const topBar = addElement(app, 'div', 'topBar')

	const title = addElement(topBar, 'h2', 'title', [], 'Todo')

	const signOut = addElement(topBar, 'button', 'signOut', [], 'Sign Out')

	// Main
	const main = addElement(app, 'div', 'main')

	// Side Bar
	const sideBar = addElement(main, 'div', 'sideBar')

	const todoLists = addElement(sideBar, 'div', 'todoLists')

	//logic to render all lists names here
	const list = addElement(todoLists, 'p', 'list', [], 'List Title')

	const addListButton = addElement(
		sideBar,
		'button',
		'addListButton',
		[],
		'Add List +'
	)

	// List View
	const listView = addElement(main, 'div', 'listView')

	const listTitle = addElement(listView, 'h1', 'listTitle', [], 'List Title') // Title list logic

	const listItems = addElement(listView, 'div', 'listItems')

	// logic to render each item like this:
	const listItem = addElement(listItems, 'div', 'listItem')

	const itemCheckbox = addElement(listItem, 'input', 'itemCheckbox', [
		['type', 'checkbox'],
	])

	const item = addElement(listItem, 'label', 'item', [], 'item') // item list logic

	const addNewItemButton = addElement(
		listView,
		'button',
		'addNewItemButton',
		[],
		'+ New Item'
	)
}

window.onload = (event) => {
	if (signedIn) {
		showListView()
	} else {
		showSignInView()
	}
}
