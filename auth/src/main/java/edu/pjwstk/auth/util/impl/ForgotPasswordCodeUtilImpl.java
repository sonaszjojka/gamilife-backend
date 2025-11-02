package edu.pjwstk.auth.util.impl;

import edu.pjwstk.auth.util.ForgotPasswordCodeUtil;
import org.apache.commons.codec.digest.DigestUtils;
import org.springframework.stereotype.Component;

import java.util.UUID;

@Component
public class ForgotPasswordCodeUtilImpl implements ForgotPasswordCodeUtil {

    @Override
    public String generateCode() {
        return UUID.randomUUID().toString();
    }

    @Override
    public String hashCode(String code) {
        return DigestUtils.sha256Hex(code);
    }
}
