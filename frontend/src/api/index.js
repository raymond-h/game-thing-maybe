import * as rxjs from 'rxjs';
import authService from './auth';

function fetchJsonObs(url, opts = {}) {
  return rxjs.defer(() => {
    const outOpts = { ...opts };

    if(authService.isAuthenticated$.value) {
      outOpts.headers = outOpts.headers || {};
      outOpts.headers['Authorization'] = `Bearer ${authService.accessToken}`;
    }

    return fetch(url, outOpts).then(res => res.json());
  });
}

const apiUrl = process.env.API_URL.replace(/{hostname}/g, location.hostname);

export function someJson() {
  return fetchJsonObs(apiUrl + '/some-json');
}
