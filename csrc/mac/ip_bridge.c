/*
   Copyright (c) Rice University
   All rights reserved.

   Licensed under the Apache License, Version 2.0 (the ""License""); you
   may not use this file except in compliance with the License. You may
   obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   THIS CODE IS PROVIDED ON AN *AS IS* BASIS, WITHOUT WARRANTIES OR
   CONDITIONS OF ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING WITHOUT
   LIMITATION ANY IMPLIED WARRANTIES OR CONDITIONS OF TITLE, FITNESS FOR
   A PARTICULAR PURPOSE, MERCHANTABLITY OR NON-INFRINGEMENT.

   See the Apache Version 2.0 License for specific language governing
   permissions and limitations under the License.
*/


/*
 *  this code writes/reads to/from a tun virtual interface and therefore creates an actual interface for users to send and receive packets over the air
 *  the following tutorial was very helpful in writing this: http://backreference.org/2010/03/26/tuntap-interface-tutorial/
 */



#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <net/if.h>
#include <linux/if_tun.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <arpa/inet.h>
#include <sys/select.h>
#include <sys/time.h>
#include <errno.h>
#include <stdarg.h>

#include "ip_bridge.h"

int tun_alloc(char *dev, int flags) {

  struct ifreq ifr;
  int fd, err;
  char *clonedev = "/dev/net/tun";

  if( (fd = open(clonedev , O_RDWR)) < 0 ) {
    perror("Opening /dev/net/tun");
    return fd;
  }

  memset(&ifr, 0, sizeof(ifr));

  ifr.ifr_flags = flags;

  if (*dev) {
    strncpy(ifr.ifr_name, dev, IFNAMSIZ);
  }

  if( (err = ioctl(fd, TUNSETIFF, (void *)&ifr)) < 0 ) {
    perror("ioctl(TUNSETIFF)");
    close(fd);
    return err;
  }

  strcpy(dev, ifr.ifr_name);

  return fd;
}

int Ndis_init(char *str)
{

	char tun_name[IFNAMSIZ];

	/* Connect to the device */
     strcpy(tun_name, str);
     tun_fd = tun_alloc(tun_name, IFF_TAP | IFF_NO_PI);  /* tun interface */

     if(tun_fd < 0){
	  perror("Allocating TUN IF");
	  exit(1);
     }

}

int WriteFragment(unsigned char * buf, int size)
{
	//write to device

    int nwrite;

    if((nwrite=write(tun_fd, buf, size)) < 0){
      perror("Writing data to TUN IF");
      exit(1);
    }
    return nwrite;
}

int ReadFragment(unsigned char * buf, int size)
{
	//read from device

    /* Note that "buffer" should be at least the MTU size of the interface, eg 1500 bytes */
	int nread;

	if((nread=read(tun_fd, buf, size)) < 0){
	  perror("Reading data from TUN IF");
	  exit(1);
	}
	return nread;


}
