package pl.gamilife.auth.service;

import pl.gamilife.auth.models.ForgotPasswordCode;

import java.util.List;
import java.util.UUID;

public interface ForgotPasswordCodeService {
    String generateAndSaveForgotPasswordCode(UUID userId);

    String hashCode(String code);

    void revokeAllActiveForgotPasswordCodesByUserId(UUID userId);

    boolean checkIfCanResendForgotPasswordCode(List<ForgotPasswordCode> codes);
}
