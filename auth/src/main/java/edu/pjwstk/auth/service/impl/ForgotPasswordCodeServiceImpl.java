package edu.pjwstk.auth.service.impl;

import edu.pjwstk.auth.service.ForgotPasswordCodeService;
import org.apache.commons.codec.digest.DigestUtils;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
public class ForgotPasswordCodeServiceImpl implements ForgotPasswordCodeService {

    @Override
    public String generateAndSaveForgotPasswordCode() {
        return UUID.randomUUID().toString();
    }

    @Override
    public String hashCode(String code) {
        return DigestUtils.sha256Hex(code);
    }

    @Override
    public void revokeAllActiveForgotPasswordCodesByUserId(UUID userId) {

    }
}
