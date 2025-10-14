package edu.pjwstk.auth.util.impl;

import edu.pjwstk.auth.util.VerificationCodeUtil;
import org.apache.commons.codec.digest.DigestUtils;
import org.springframework.stereotype.Component;

@Component
public class VerificationCodeUtilImpl implements VerificationCodeUtil {
    @Override
    public String hashCode(String code) {
        return DigestUtils.sha256Hex(code);
    }
}
