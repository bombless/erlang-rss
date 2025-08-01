const tls = require('tls');
const zlib = require('zlib');

const options = {
  host: 'news.sina.com.cn',
  port: 443,
  servername: 'news.sina.com.cn', // 用于SNI
};

const req = [
  'GET / HTTP/1.1',
  'Host: news.sina.com.cn',
  'User-Agent: Mozilla/5.0',
  'Accept-Encoding: gzip',
  'Connection: close',
  '',
  '',
].join('\r\n');

const socket = tls.connect(options, () => {
  socket.write(req);
});

let buffers = [];

socket.on('data', (chunk) => {
  buffers.push(chunk);
});

socket.on('end', () => {
  const data = Buffer.concat(buffers);
  const headerEnd = data.indexOf('\r\n\r\n');
  if (headerEnd === -1) {
    console.error('Invalid HTTP response');
    return;
  }

  const bodyStart = headerEnd + 4;
  const headers = data.slice(0, bodyStart).toString();
  const body = data.slice(bodyStart);

  // 检查是否 gzip 压缩
  if (headers.includes('Content-Encoding: gzip')) {
    zlib.gunzip(body, (err, decompressed) => {
      if (err) {
        console.error('解压失败:', err);
      } else {
        process.stdout.write(decompressed);
      }
    });
  } else {
    process.stdout.write(body);
  }
});

socket.on('error', (err) => {
  console.error('连接错误:', err);
});
