#!/usr/bin/env node

const socket = require('socket.io-client')('http://localhost:9002');
const someDelay = 100;
socket.on('connect', function () {
    console.log('connected...');
    if (process.argv[2] && process.argv[3]) {
        console.log('sending ' + process.argv[2] + ': ' + process.argv[3]);
        socket.emit(process.argv[2], process.argv[3]);
        setTimeout(() => {
            process.exit(0);  
        }, someDelay);
    } else {
        console.log('usage: ./client.js <event> <data>');
        process.exit(1);
    }
});