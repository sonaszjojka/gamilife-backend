package edu.pjwstk.common.userApi;

import edu.pjwstk.common.userApi.dto.BasicUserInfoApiDto;
import edu.pjwstk.common.userApi.dto.CheckIfUsersEmailIsVerifiedApiDto;
import edu.pjwstk.common.userApi.dto.RegisterUserApiDto;
import edu.pjwstk.common.userApi.dto.SecureUserInfoApiDto;

import java.util.Optional;
import java.util.UUID;

public interface UserApi {
    BasicUserInfoApiDto registerNewUser(RegisterUserApiDto user);

    Optional<BasicUserInfoApiDto> getUserById(UUID userId);

    Optional<BasicUserInfoApiDto> getUserByEmail(String email);

    Optional<SecureUserInfoApiDto> getSecureUserDataById(UUID userId);

    Optional<SecureUserInfoApiDto> getSecureUserDataByEmail(String email);

    void updateUserEmail(UUID userId, String newEmail);

    CheckIfUsersEmailIsVerifiedApiDto checkIfUsersEmailIsVerified(UUID userId);

    BasicUserInfoApiDto confirmUserEmailVerification(UUID userId);

    void resetUserPassword(UUID userId, String hashedNewPassword);
}
