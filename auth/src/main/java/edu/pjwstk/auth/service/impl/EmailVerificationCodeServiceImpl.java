package edu.pjwstk.auth.service.impl;

import edu.pjwstk.auth.repository.JpaEmailVerificationRepository;
import edu.pjwstk.auth.service.EmailVerificationCodeService;
import lombok.AllArgsConstructor;
import org.apache.commons.codec.digest.DigestUtils;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@AllArgsConstructor
public class EmailVerificationCodeServiceImpl implements EmailVerificationCodeService {

    private final JpaEmailVerificationRepository emailVerificationRepository;

    @Override
    public String generateAndSaveEmailVerificationCode(UUID userId) {
        return null;
    }

    @Override
    public String hashCode(String code) {
        return DigestUtils.sha256Hex(code);
    }

    @Override
    public void revokeAllActiveEmailVerificationCodesByUserId(UUID userId) {
        emailVerificationRepository.revokeAllActiveEmailVerificationCodesByUserId(userId);
    }
}
