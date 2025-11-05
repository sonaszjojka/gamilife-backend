package edu.pjwstk.user.usecase;

import edu.pjwstk.api.user.dto.SecureUserInfoApiDto;

import java.util.Optional;

public interface GetSecureUserDataByEmailUseCase {
    Optional<SecureUserInfoApiDto> execute(String email);
}
