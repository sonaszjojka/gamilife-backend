package edu.pjwstk.user.usecase;

import edu.pjwstk.common.userApi.dto.SecureUserInfoApiDto;

import java.util.Optional;
import java.util.UUID;

public interface GetSecureUserDataByIdUseCase {
    Optional<SecureUserInfoApiDto> execute(UUID userId);
}
