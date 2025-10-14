package edu.pjwstk.user.usecase;

import edu.pjwstk.common.userApi.dto.SecureUserInfoApiDto;

import java.util.Optional;

public interface GetSecureUserDataByEmailUseCase {
    Optional<SecureUserInfoApiDto> execute(String email);
}
