package pl.gamilife.auth.domain.service;

import pl.gamilife.auth.domain.model.ForgotPasswordCode;

import java.util.List;
import java.util.UUID;

public interface ForgotPasswordCodeService {
    String generateAndSaveForgotPasswordCode(UUID userId);

    String hashCode(String code);

    void revokeAllActiveForgotPasswordCodesByUserId(UUID userId);

    boolean checkIfCanResendForgotPasswordCode(List<ForgotPasswordCode> codes);
}
