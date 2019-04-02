import Pusher from 'pusher-js';

import * as api from './index';

Pusher.logToConsole = true;

export default new Pusher(process.env.PUSHER_APP_KEY, {
  cluster: process.env.PUSHER_CLUSTER,

  authorizer(chan, _opts) {
    return {
      authorize(socketId, cb) {
        api.authenticatePusher(chan, socketId)
          .toPromise()
          .then(v => cb(null, v), err => cb(err));
      }
    };
  }
});
