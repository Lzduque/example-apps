// Mock for view
const signedIn = true

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

	// Horizontal line
	const horizontalLine = addElement(app, 'div', 'horizontalLine')

	// Main
	const main = addElement(app, 'div', 'main')

	// Side Bar
	const sideBar = addElement(main, 'div', 'sideBar')

	// Vertical line
	const verticalLine = addElement(main, 'div', 'verticalLine')

	const listsContainer = addElement(sideBar, 'div', 'listsContainer')

	//logic to render all lists names here
	const receivedMessageLists = {
		type: 'get-all-lists',
		lists: [
			{id: 'list1', title: 'First List'},
			{id: 'list2', title: 'Second List'},
			{id: 'list3', title: 'Third List'},
			{id: 'list4', title: 'Third List'},
			{id: 'list5', title: 'Third List'},
			{id: 'list6', title: 'Third List'},
			{id: 'list7', title: 'Third List'},
		],
	}

	receivedMessageLists.lists.forEach(({id, title}) => {
		const listContainer = addElement(listsContainer, 'div', id)
		const list = addElement(listContainer, 'p', `${id}ListTitle`, [], title)
		const deleteButton = addElement(
			listContainer,
			'button',
			`${id}DeleteButton`,
			[],
			'Delete'
		)
	})

	const addListButton = addElement(
		sideBar,
		'button',
		'addListButton',
		[],
		'Add List +'
	)

	// List View
	const listView = addElement(main, 'div', 'listView')

	const listTitleContainer = addElement(listView, 'div', 'listTitleContainer')
	const listItems = addElement(listView, 'div', 'listItems')

	const sentMessageList1 = {
		type: 'get-list-with-items',
		listID: 'list1',
	}
	const receivedMessageList1 = {
		type: 'list-with-items',
		list: {
			id: 'list1',
			title: 'First List',
		},
		items: [
			{id: 'item1', item: 'do the laundry'},
			{id: 'item2', item: 'grocery shopping'},
			{id: 'item3', item: 'clean littler box'},
		],
	}

	const listTitle = addElement(
		listTitleContainer,
		'h1',
		`${receivedMessageList1.list.id}Title`,
		[],
		receivedMessageList1.list.title
	)

	receivedMessageList1.items.forEach(({id, item}) => {
		const itemContainer = addElement(listItems, 'div', id)
		const itemCheckbox = addElement(
			itemContainer,
			'input',
			`${id}Checkbox`,
			[['type', 'checkbox']]
		)
		const text = addElement(itemContainer, 'label', `${id}Text`, [], item)
		const deleteButton = addElement(
			itemContainer,
			'button',
			`${id}DeleteButton`,
			[],
			'Delete'
		)
	})

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
