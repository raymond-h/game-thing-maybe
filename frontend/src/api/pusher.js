import Pusher from 'pusher-js';

Pusher.logToConsole = true;

export default new Pusher(process.env.PUSHER_APP_KEY, {
  cluster: process.env.PUSHER_CLUSTER
});
