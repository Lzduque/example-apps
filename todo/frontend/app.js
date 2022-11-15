// Mock for view
const signedIn = true

const showSignInView = () => {
	const app = document.getElementById('app')

	// REGISTER
	const register = document.createElement('h2')
	register.setAttribute('id', 'register')
	register.innerText = 'Register'
	app.append(register)

	const registerDiv = document.createElement('div')
	registerDiv.setAttribute('id', 'register-div')
	app.append(registerDiv)

	const emailDiv1 = document.createElement('div')
	emailDiv1.setAttribute('id', 'email-div1')
	registerDiv.append(emailDiv1)

	const emailLabel1 = document.createElement('label')
	emailLabel1.setAttribute('id', 'email-label1')
	emailLabel1.innerText = 'Email'
	emailDiv1.append(emailLabel1)

	const emailField1 = document.createElement('input')
	emailField1.setAttribute('id', 'email-field1')
	emailField1.setAttribute('type', 'email')
	emailDiv1.append(emailField1)

	const passwordDiv1 = document.createElement('div')
	passwordDiv1.setAttribute('id', 'password-div1')
	registerDiv.append(passwordDiv1)

	const passwordLabel1 = document.createElement('label')
	passwordLabel1.setAttribute('id', 'password-label1')
	passwordLabel1.innerText = 'Password'
	passwordDiv1.append(passwordLabel1)

	const passwordField1 = document.createElement('input')
	passwordField1.setAttribute('id', 'password-field1')
	passwordField1.setAttribute('type', 'password')
	passwordDiv1.append(passwordField1)

	const submitButton1 = document.createElement('button')
	submitButton1.setAttribute('id', 'submit-button')
	submitButton1.innerText = 'Submit'
	registerDiv.append(submitButton1)

	// SIGN IN
	const signin = document.createElement('h2')
	signin.setAttribute('id', 'signin')
	signin.innerText = 'Sign In'
	app.append(signin)

	const signinDiv = document.createElement('div')
	signinDiv.setAttribute('id', 'signin-div')
	app.append(signinDiv)

	const emailDiv2 = document.createElement('div')
	emailDiv2.setAttribute('id', 'email-div2')
	signinDiv.append(emailDiv2)

	const emailLabel2 = document.createElement('label')
	emailLabel2.setAttribute('id', 'email-label2')
	emailLabel2.innerText = 'Email'
	emailDiv2.append(emailLabel2)

	const emailField2 = document.createElement('input')
	emailField2.setAttribute('id', 'email-field2')
	emailField2.setAttribute('type', 'email')
	emailDiv2.append(emailField2)

	const passwordDiv2 = document.createElement('div')
	passwordDiv2.setAttribute('id', 'password-div2')
	signinDiv.append(passwordDiv2)

	const passwordLabel2 = document.createElement('label')
	passwordLabel2.setAttribute('id', 'password-label2')
	passwordLabel2.innerText = 'Password'
	passwordDiv2.append(passwordLabel2)

	const passwordField2 = document.createElement('input')
	passwordField2.setAttribute('id', 'password-field2')
	passwordField2.setAttribute('type', 'password')
	passwordDiv2.append(passwordField2)

	const submitButton2 = document.createElement('button')
	submitButton2.setAttribute('id', 'submit-button')
	submitButton2.innerText = 'Submit'
	signinDiv.append(submitButton2)
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
