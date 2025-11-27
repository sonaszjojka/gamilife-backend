package edu.pjwstk.user.usecase;

import edu.pjwstk.api.user.dto.SecureUserInfoApiDto;

import java.util.Optional;
import java.util.UUID;

public interface GetSecureUserDataByIdUseCase {
    Optional<SecureUserInfoApiDto> execute(UUID userId);
}
