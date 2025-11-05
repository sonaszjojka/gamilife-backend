package edu.pjwstk.user.api;

import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.api.user.dto.BasicUserInfoApiDto;
import edu.pjwstk.api.user.dto.CheckIfUsersEmailIsVerifiedApiDto;
import edu.pjwstk.api.user.dto.RegisterUserApiDto;
import edu.pjwstk.api.user.dto.SecureUserInfoApiDto;
import edu.pjwstk.user.usecase.*;
import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.Optional;
import java.util.UUID;

@Service
@AllArgsConstructor
public class UserApiImpl implements UserApi {

    private final CheckIfUsersEmailIsVerifiedUseCase checkIfUsersEmailIsVerifiedUseCase;
    private final ConfirmUserEmailVerificationUseCase confirmUserEmailVerificationUseCase;
    private final GetSecureUserDataByEmailUseCase getSecureUserDataByEmailUseCase;
    private final GetSecureUserDataByIdUseCase getSecureUserDataByIdUseCase;
    private final GetUserByEmailUseCase getUserByEmailUseCase;
    private final GetUserByIdUseCase getUserByIdUseCase;
    private final RegisterNewUserUseCase registerNewUserUseCase;
    private final UpdateUserEmailUseCase updateUserEmailUseCase;
    private final ResetUserPasswordUseCase resetUserPasswordUseCase;

    @Override
    @Transactional
    public BasicUserInfoApiDto registerNewUser(RegisterUserApiDto user) {
        return registerNewUserUseCase.execute(user);
    }

    @Override
    public Optional<BasicUserInfoApiDto> getUserById(UUID userId) {
        return getUserByIdUseCase.execute(userId);
    }

    @Override
    public Optional<BasicUserInfoApiDto> getUserByEmail(String email) {
        return getUserByEmailUseCase.execute(email);
    }

    @Override
    public Optional<SecureUserInfoApiDto> getSecureUserDataById(UUID userId) {
        return getSecureUserDataByIdUseCase.execute(userId);
    }

    @Override
    public Optional<SecureUserInfoApiDto> getSecureUserDataByEmail(String email) {
        return getSecureUserDataByEmailUseCase.execute(email);
    }

    @Override
    public void updateUserEmail(UUID userId, String newEmail) {
        updateUserEmailUseCase.execute(userId, newEmail);
    }

    @Override
    public CheckIfUsersEmailIsVerifiedApiDto checkIfUsersEmailIsVerified(UUID userId) {
        return checkIfUsersEmailIsVerifiedUseCase.execute(userId);
    }

    public BasicUserInfoApiDto confirmUserEmailVerification(UUID userId) {
        return confirmUserEmailVerificationUseCase.execute(userId);
    }

    @Override
    public void resetUserPassword(UUID userId, String hashedNewPassword) {
        resetUserPasswordUseCase.execute(userId, hashedNewPassword);
    }
}
