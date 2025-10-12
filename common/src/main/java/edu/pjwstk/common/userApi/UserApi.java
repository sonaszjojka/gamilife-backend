package edu.pjwstk.common.userApi;

import edu.pjwstk.common.userApi.dto.BasicUserInfoApiDto;
import edu.pjwstk.common.userApi.dto.RegisterUserApiDto;

import java.util.Optional;
import java.util.UUID;

public interface UserApi {
    BasicUserInfoApiDto registerNewUser(RegisterUserApiDto user);

    Optional<BasicUserInfoApiDto> getUserById(UUID userId);

    Optional<BasicUserInfoApiDto> getUserByEmail(String email);

    void updateUserEmail(UUID userId, String newEmail);
}
