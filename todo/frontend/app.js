const socket = new WebSocket('ws://127.0.0.1:9160')

socket.onopen = (event) => {
	socket.send(
		JSON.stringify({
			type_: 'ReqConnection',
		})
	)
	console.log('[open] Connection established')
	console.log('Sending to server')
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
					reqTodoListSessionId: localStorage.getItem('sessionId'),
				})
			)
			break
		case 'ResRegister':
			localStorage.setItem('sessionId', message.resRegisterSessionId)
			socket.send(
				JSON.stringify({
					type_: 'ReqTodoList',
					reqTodoListSessionId: message.resRegisterSessionId,
				})
			)
			break
		case 'ResSignIn':
			localStorage.setItem('sessionId', message.resSignInSessionId)
			socket.send(
				JSON.stringify({
					type_: 'ReqTodoList',
					reqTodoListSessionId: message.resSignInSessionId,
				})
			)
			break
		case 'ResSignOut':
			localStorage.removeItem('sessionId')
			showSignInView()
			break
		case 'UpdateTodoList':
			console.log(`[message] UpdateTodoList`)
			updateTodoList(message.items)
			break
		case 'ResTodoList':
			console.log(`[message] ResTodoList`)
			showListView()
			updateTodoList(message.items)
			break
		case 'ResCreateTodo':
			socket.send(
				JSON.stringify({
					type_: 'ReqTodoList',
					reqTodoListSessionId: localStorage.getItem('sessionId'),
				})
			)
			break
		case 'ResDeleteTodo':
			socket.send(
				JSON.stringify({
					type_: 'ReqTodoList',
					reqTodoListSessionId: localStorage.getItem('sessionId'),
				})
			)
			break
		case 'ResToggleTodo':
			socket.send(
				JSON.stringify({
					type_: 'ReqTodoList',
					reqTodoListSessionId: localStorage.getItem('sessionId'),
				})
			)
			break
		case 'ErrorRegisterEmail':
			console.log(message.text)
			emailError1.innerText = message.text
			break
		case 'ErrorRegisterPassword':
			console.log(message.text)
			passwordError1.innerText = message.text
			break
		case 'ErrorSignIn':
			console.log(message.text)
			signInError.innerText = message.text
			break
		case 'ErrorCreateTodo':
			console.log(message.text)
			todoError.innerText = message.text
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

const showSignInView = () => {
	const app = document.getElementById('app')
	app.innerHTML = ''

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

	const emailError1 = addElement(registerContainer, 'p', 'emailError1', [])

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

	const passwordError1 = addElement(
		registerContainer,
		'p',
		'passwordError1',
		[]
	)

	emailField1.addEventListener(
		'input',
		(event) => (emailError1.innerText = '')
	)
	passwordField1.addEventListener(
		'input',
		(event) => (emailError1.innerText = '')
	)

	emailField1.addEventListener(
		'input',
		(event) => (passwordError1.innerText = '')
	)
	passwordField1.addEventListener(
		'input',
		(event) => (passwordError1.innerText = '')
	)

	const submitButton1 = addElement(
		registerContainer,
		'button',
		'submitButton1',
		[],
		'Submit'
	)
	const sendRegister = (event) => {
		try {
			socket.send(
				JSON.stringify({
					type_: 'ReqRegister',
					reqRegisterEmail: emailField1.value,
					reqRegisterPassword: passwordField1.value,
				})
			)
		} catch (e) {
			console.log('ERROR ReqRegister: ', e)
		}
	}
	submitButton1.addEventListener('click', sendRegister)
	emailField1.addEventListener(
		'keydown',
		(event) => event.key === 'Enter' && sendRegister()
	)
	passwordField1.addEventListener(
		'keydown',
		(event) => event.key === 'Enter' && sendRegister()
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

	const signInError = addElement(signInContainer, 'p', 'signInError')
	emailField2.addEventListener(
		'input',
		(event) => (signInError.innerText = '')
	)
	passwordField2.addEventListener(
		'input',
		(event) => (signInError.innerText = '')
	)

	const submitButton2 = addElement(
		signInContainer,
		'button',
		'submitButton2',
		[],
		'Submit'
	)
	const sendSignIn = (event) => {
		try {
			socket.send(
				JSON.stringify({
					type_: 'ReqSignIn',
					reqSignInEmail: emailField2.value,
					reqSignInPassword: passwordField2.value,
				})
			)
		} catch (e) {
			console.log('ERROR ReqSignIn: ', e)
		}
	}
	submitButton2.addEventListener('click', sendSignIn)
	emailField2.addEventListener(
		'keydown',
		(event) => event.key === 'Enter' && sendSignIn()
	)
	passwordField2.addEventListener(
		'keydown',
		(event) => event.key === 'Enter' && sendSignIn()
	)
}

const showListView = () => {
	const app = document.getElementById('app')
	app.innerHTML = ''

	// Top Bar
	const topBar = addElement(app, 'div', 'topBar')

	const title = addElement(topBar, 'h2', 'title', [], 'Todos')

	const signOut = () => {
		try {
			socket.send(
				JSON.stringify({
					type_: 'ReqSignOut',
					reqSignOutSessionId: localStorage.getItem('sessionId'),
				})
			)
		} catch (e) {
			console.log('ERROR ReqSignOut: ', e)
		}
	}

	const signOutButton = addElement(
		topBar,
		'button',
		'signOut',
		[],
		'Sign Out'
	)
	signOutButton.addEventListener('click', signOut)

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

	const todoError = addElement(main, 'p', 'todoError')
	newItemBox.addEventListener('input', (event) => (todoError.innerText = ''))

	const createTodo = () => {
		try {
			socket.send(
				JSON.stringify({
					type_: 'ReqCreateTodo',
					name: newItemBox.value,
					reqCreateTodoSessionId: localStorage.getItem('sessionId'),
				})
			)
			newItemBox.value = ''
		} catch (e) {
			console.log('ERROR ReqCreateTodo: ', e)
		}
	}

	newItemBox.addEventListener(
		'keydown',
		(event) => event.key === 'Enter' && createTodo()
	)

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

const updateTodoList = (items) => {
	if (!listView) return
	listView.innerHTML = ''
	items.forEach(({id, name, checked}) => {
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
			try {
				socket.send(
					JSON.stringify({
						type_: 'ReqToggleTodo',
						reqToggleTodoId: id,
						checked: itemCheckbox.checked,
						reqToggleTodoSessionId:
							localStorage.getItem('sessionId'),
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
						reqDeleteTodoId: id,
						reqDeleteTodoSessionId:
							localStorage.getItem('sessionId'),
					})
				)
			} catch (e) {
				console.log('ERROR ReqDeleteTodo: ', e)
			}
		}
		deleteButton.addEventListener('click', deleteTodo(id))
	})
}

window.onload = (event) => {
	// const sessionId = localStorage.getItem('sessionId')
	// socket.send(
	// 	JSON.stringify({
	// 		type_: 'ReqResumeSession',
	// 		reqResumeSessionSessionId: sessionId,
	// 	})
	// )
}
