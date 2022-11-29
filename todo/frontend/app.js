// type message = {
// 	type:: string,
// 	...
// }

//  type messageConnection = {
//		type_ = 'in-connection', :: string
// 		userId :: string
// }
const socket = new WebSocket('ws://127.0.0.1:9160')

socket.onopen = (event) => {
	console.log('[open] Connection established')
	console.log('Sending to server')
	socket.send(
		JSON.stringify({
			type_: 'ReqConnection',
			userId: '1234',
		})
	)
}

socket.onmessage = (event) => {
	const message = JSON.parse(event.data)
	console.log(`[message] Data received from server: `)
	console.log(message)
	switch (message.type_) {
		case 'ResConnection':
			console.log(`[message] Connection with server established!`)
			socket.send(
				JSON.stringify({
					type_: 'ReqTodoList',
				})
			)
			break
		case 'ResTodoList':
			listView.innerHTML = ''
			message.items.forEach(({id, name, checked}) => {
				console.log(`[message] ResTodoList`)

				const itemContainer = addElement(listView, 'div', id)
				const itemLeftContainer = addElement(
					itemContainer,
					'div',
					`${id}LeftContainer`
				)
				const checkedAttr = checked ? ['checked', ''] : null
				const itemCheckboxAttrs = [['type', 'checkbox'], checkedAttr]
				const itemCheckbox = addElement(
					itemLeftContainer,
					'input',
					`${id}Checkbox`,
					itemCheckboxAttrs.filter(isTruthy)
				)
				const toggleTodo = (id) => (event) => {
					console.log('event.target.checked: ', event.target.checked)
					try {
						socket.send(
							JSON.stringify({
								type_: 'ReqToggleTodo',
								id: id,
								checked: event.target.checked,
							})
						)
					} catch (e) {
						console.log('ERROR ReqToggleTodo: ', e)
					}
				}
				itemCheckbox.addEventListener('change', toggleTodo(id))
				const text = addElement(
					itemLeftContainer,
					'label',
					`${id}Text`,
					[],
					name
				)
				const deleteButton = addElement(
					itemContainer,
					'button',
					`${id}DeleteButton`,
					[],
					'Delete'
				)
				const deleteTodo = (id) => (event) => {
					try {
						socket.send(
							JSON.stringify({
								type_: 'ReqDeleteTodo',
								id: id,
							})
						)
					} catch (e) {
						console.log('ERROR ReqDeleteTodo: ', e)
					}
				}
				deleteButton.addEventListener('click', deleteTodo(id))
			})
			break
		case 'ResCreateTodo':
			socket.send(
				JSON.stringify({
					type_: 'ReqTodoList',
				})
			)
			break
		case 'ResDeleteTodo':
			socket.send(
				JSON.stringify({
					type_: 'ReqTodoList',
				})
			)
			break
		case 'ResToggleTodo':
			socket.send(
				JSON.stringify({
					type_: 'ReqTodoList',
				})
			)
			break
		default:
			console.log('Message not recognized')
			break
	}
}

socket.onclose = (event) => {
	if (event.wasClean) {
		console.log(
			`[close] Connection closed cleanly, code=${event.code} reason=${event.reason}`
		)
	} else {
		// e.g. server process killed or network down
		// event.code is usually 1006 in this case
		console.log('[close] Connection died')
	}
}

socket.onerror = (error) => {
	console.log(`[error]: ${error}`)
}

// Mock for view
const signedIn = true

const showSignInView = () => {
	const app = document.getElementById('app')

	const signInViewContainer = addElement(app, 'div', 'signInViewContainer')

	// REGISTER
	const registerContainer = addElement(
		signInViewContainer,
		'div',
		'registerContainer'
	)

	const register = addElement(
		registerContainer,
		'h2',
		'register',
		[],
		'Register'
	)

	const emailDiv1 = addElement(registerContainer, 'div', 'emailDiv1')

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

	const passwordDiv1 = addElement(registerContainer, 'div', 'passwordDiv1')

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
		registerContainer,
		'button',
		'submitButton1',
		[],
		'Submit'
	)

	// Horizontal line
	const horizontalLine = addElement(
		signInViewContainer,
		'div',
		'horizontalLine'
	)

	// SIGN IN
	const signInContainer = addElement(
		signInViewContainer,
		'div',
		'signInContainer'
	)

	const signin = addElement(signInContainer, 'h2', 'signin', [], 'Sign In')

	const emailDiv2 = addElement(signInContainer, 'div', 'emailDiv2')

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

	const passwordDiv2 = addElement(signInContainer, 'div', 'passwordDiv2')

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
		signInContainer,
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

	const title = addElement(topBar, 'h2', 'title', [], 'Todos')

	const signOut = addElement(topBar, 'button', 'signOut', [], 'Sign Out')

	// Horizontal line
	const horizontalLine = addElement(app, 'div', 'horizontalLine')

	// Main
	const main = addElement(app, 'div', 'main')

	// Title
	const listTitleContainer = addElement(main, 'div', 'listTitleContainer')

	// New Item Box
	const newItemContainer = addElement(main, 'div', 'newItemContainer')

	const newItemBox = addElement(newItemContainer, 'input', 'newItemBox', [
		['type', 'text'],
	])

	const createTodo = () => {
		try {
			socket.send(
				JSON.stringify({
					type_: 'ReqCreateTodo',
					name: newItemBox.value,
				})
			)
			newItemBox.value = ''
		} catch (e) {
			console.log('ERROR ReqCreateTodo: ', e)
		}
	}

	const addNewItemButton = addElement(
		newItemContainer,
		'button',
		'addNewItemButton',
		[],
		'+'
	)

	addNewItemButton.addEventListener('click', createTodo)

	// List View
	const listView = addElement(main, 'div', 'listView')

	const listTitle = addElement(
		listTitleContainer,
		'h1',
		`listTitle`,
		[],
		'My Tasks'
	)
}

window.onload = (event) => {
	if (signedIn) {
		showListView()
	} else {
		showSignInView()
	}
}
