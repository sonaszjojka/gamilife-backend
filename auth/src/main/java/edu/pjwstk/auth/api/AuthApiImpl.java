package edu.pjwstk.auth.api;

import edu.pjwstk.auth.dto.service.ChangePasswordInternalCommand;
import edu.pjwstk.auth.usecase.ChangePasswordUseCase;
import edu.pjwstk.auth.usecase.GetAuthenticatedUserDataUseCase;
import edu.pjwstk.auth.usecase.RotateUserTokensUseCase;
import edu.pjwstk.common.authApi.AuthApi;
import edu.pjwstk.common.authApi.dto.AuthTokens;
import edu.pjwstk.common.authApi.dto.ChangePasswordCommand;
import edu.pjwstk.common.authApi.dto.CurrentUserDto;
import edu.pjwstk.common.authApi.dto.RotateUserTokensCommand;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.Optional;

@Service
@AllArgsConstructor
public class AuthApiImpl implements AuthApi {

    private GetAuthenticatedUserDataUseCase getAuthenticatedUserDataUseCase;
    private ChangePasswordUseCase changePasswordUseCase;
    private RotateUserTokensUseCase rotateUserTokensUseCase;

    @Override
    public Optional<CurrentUserDto> getCurrentUser() {
        return getAuthenticatedUserDataUseCase.execute();
    }

    @Override
    public String handleChangePassword(ChangePasswordCommand dto) {
        return changePasswordUseCase.execute(new ChangePasswordInternalCommand(
                dto.providedPassword(),
                dto.hashedUserPassword(),
                dto.newPassword()
        ));
    }

    @Override
    public AuthTokens rotateUserTokens(RotateUserTokensCommand rotateUserTokensCommand) {
        return rotateUserTokensUseCase.execute(rotateUserTokensCommand);
    }

}
