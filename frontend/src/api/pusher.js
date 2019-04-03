import Pusher from 'pusher-js';

import * as api from './index';

Pusher.logToConsole = true;

export const pusher = new Pusher(process.env.PUSHER_APP_KEY, {
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

class ChannelPool {
  constructor(pusher) {
    this._channels = {};
    this.pusher = pusher;
  }

  get(channelName) {
    const chan = this.pusher.subscribe(channelName);
    this._channels[channelName] = (this._channels[channelName] || 0) + 1;
    return chan;
  }

  release(channelName) {
    if(this._channels[channelName] > 0) {
      this._channels[channelName] = (this._channels[channelName] || 0) - 1;

      if(this._channels[channelName] === 0) {
        this.pusher.unsubscribe(channelName);
      }
    }
  }
}

export const channelPool = new ChannelPool(pusher);

export default pusher;
