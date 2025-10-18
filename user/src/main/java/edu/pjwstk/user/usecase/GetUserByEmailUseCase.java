package edu.pjwstk.user.usecase;

import edu.pjwstk.common.userApi.dto.BasicUserInfoApiDto;

import java.util.Optional;

public interface GetUserByEmailUseCase {
    Optional<BasicUserInfoApiDto> execute(String email);
}
