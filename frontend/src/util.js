import * as rxjs from 'rxjs';
import { bufferCount } from 'rxjs/operators';
import _ from 'lodash';

export function forkByKey(fn) {
  return obs => new rxjs.Observable(subscriber => {
    let subs = {};
    const unsubAll = () => {
      for(const key in subs) {
        subs[key].unsubscribe();
      }
      subs = {};
    };

    const outerSub = obs
      .pipe(
        bufferCount(2, 1)
      )
      .subscribe({
        next([oldArr, newArr]) {
          const addedKeys = _.difference(newArr, oldArr);
          const removedKeys = _.difference(oldArr, newArr);

          for(const key of addedKeys) {
            const obs = rxjs.from(fn(key));
            subs[key] = obs.subscribe({
              next(val) {
                subscriber.next(val);
              },

              error(err) {
                unsubAll();
                subscriber.error(err);
              },

              complete() {
                // clean up to prevent potential memory leak
                delete subs[key];
              }
            });
          }

          for(const key of removedKeys) {
            if(subs[key] != null) {
              subs[key].unsubscribe();
              delete subs[key];
            }
          }
        },

        error(err) {
          unsubAll();
          subscriber.error(err);
        },

        complete() {
          unsubAll();
          subscriber.complete();
        }
      });

    return () => {
      unsubAll();
      outerSub.unsubscribe();
    };
  });
}
