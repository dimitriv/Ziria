#include <stdio.h>
#include "vcpdriver/vcpdriver_api.h"
#include "vitc/vitc_api.h"
#include "vitdec/vitdec_profile.h"
#include "vitc/spiralf72.h"
#include <ti/csl/csl_tsc.h>


Vitdec_Profile vdp;
VCP2_ConfigIc vcpConfig;
void __ext_atomix_viterbi_init(int frame_len, Uint16 code_rate) {
	vcp2_initOnce(); // shouldn't be here
	Uint64 outputParams;
	vitdec_profile_wifiGee_populate_convergent(&vdp);
	vdp.rate = code_rate;
	vdp.frameLen = frame_len;
	vcp2_genConf(&vdp, &vcpConfig);
	vcp2_initPerUse(&vcpConfig, &outputParams,
					vdp.inputBM_frames, vdp.outputHD_bytes_rounded);

}

void __ext_atomix_viterbi_decode(Uint32* branch_metric, int len1, Uint32* hard_decision, int len2) {
	int len = vdp.frameLen + vdp.c;
	volatile Uint32 cword0 = 0;
	volatile Uint64 timestamps[20];
	Uint32 vcpi0 = 0;
	volatile CSL_Uint64 tp1;
	vcp2_decode(vcpi0, &vcpConfig, branch_metric, hard_decision,
					vdp.inputBM_frames, vdp.outputHD_bytes_rounded, &cword0, timestamps);
	vcp2_waitForCompletion(len, vcpi0, &tp1, BREAKTYPE_INT, timestamps);
	vcp2_close();
}
