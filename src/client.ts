import { createClientWorld } from './world/client';
import { execute, parse, tokenize } from './core/lisp';
import { toString } from './core/toString';

const body = document.body

const left = document.createElement('div')
left.classList.add('left')
const editor = document.createElement('textarea')
left.appendChild(editor)
body.appendChild(left)

const right = document.createElement('div')
right.classList.add('right')
const output = document.createElement('pre')
right.appendChild(output)
body.appendChild(right)

const button = document.createElement('button')
button.style.position = 'absolute'
button.style.bottom = '1em'
button.style.right = '1em'
button.style.right = '1em'
button.innerHTML = 'Evaluate'

body.appendChild(button)


window.addEventListener('load', async () => {
  const env = await createClientWorld();

  button.onclick = async (e) => {

    const r = await execute(editor.value, env)
    // console.log('evaluated:', toString(r))

    output.innerHTML = toString(r)
  }
})
