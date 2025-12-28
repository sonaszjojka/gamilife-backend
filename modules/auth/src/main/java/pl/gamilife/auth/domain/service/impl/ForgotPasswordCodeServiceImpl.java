package pl.gamilife.auth.domain.service.impl;

import lombok.RequiredArgsConstructor;
import org.apache.commons.codec.digest.DigestUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import pl.gamilife.auth.domain.model.ForgotPasswordCode;
import pl.gamilife.auth.domain.port.repository.ForgotPasswordCodeRepository;
import pl.gamilife.auth.domain.service.ForgotPasswordCodeService;

import java.time.Instant;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class ForgotPasswordCodeServiceImpl implements ForgotPasswordCodeService {

    private final ForgotPasswordCodeRepository forgotPasswordCodeRepository;

    @Value("${spring.codes.forgot-password-code.expires-in}")
    private long forgotPasswordCodeTimeout;

    @Value("${spring.codes.forgot-password-code.resend-interval}")
    private long forgotPasswordCodeResendInterval;

    @Override
    public String generateAndSaveForgotPasswordCode(UUID userId) {
        String code = UUID.randomUUID().toString();

        ForgotPasswordCode forgotPasswordCode = ForgotPasswordCode.create(
                userId,
                hashCode(code),
                forgotPasswordCodeTimeout
        );

        forgotPasswordCodeRepository.save(forgotPasswordCode);

        return code;
    }

    @Override
    public String hashCode(String code) {
        return DigestUtils.sha256Hex(code);
    }

    @Override
    public void revokeAllActiveForgotPasswordCodesByUserId(UUID userId) {
        forgotPasswordCodeRepository.revokeAllActiveForgotPasswordCodesByUserId(userId);
    }

    @Override
    public boolean checkIfCanResendForgotPasswordCode(List<ForgotPasswordCode> codes) {
        // Do not resend a code if user has a non revoked code that will not expire
        // in the duration of the resend interval period
        return codes.isEmpty() ||
                codes.getFirst()
                        .getExpiresAt()
                        .minusSeconds(forgotPasswordCodeResendInterval)
                        .isBefore(Instant.now());
    }


}
