
// from https://github.com/plibither8/getchar/blob/master/index.js (MIT Licensed)
export const getchar = async (encoding: BufferEncoding = 'ascii'): Promise<string> => {

  process.stdin.setRawMode(true);
  process.stdin.setEncoding('utf8');

  return new Promise(resolve => process.stdin.once('data', data => {

      const byteArray = [...data];
      if (byteArray.length > 0 && byteArray[0] === 3) {
          console.log('^C');
          process.exit(0);
      }
      process.stdin.setRawMode(false);
      resolve(data.toString(encoding));

  }));
};
