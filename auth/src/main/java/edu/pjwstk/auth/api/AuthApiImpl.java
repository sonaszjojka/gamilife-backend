package edu.pjwstk.auth.api;

import edu.pjwstk.api.auth.dto.AuthTokens;
import edu.pjwstk.auth.usecase.command.ChangePasswordInternalCommand;
import edu.pjwstk.auth.usecase.ChangePasswordUseCase;
import edu.pjwstk.auth.usecase.GetAuthenticatedUserDataUseCase;
import edu.pjwstk.auth.usecase.RotateUserTokensUseCase;
import edu.pjwstk.api.auth.AuthApi;
import edu.pjwstk.api.auth.dto.ChangePasswordCommand;
import edu.pjwstk.api.auth.dto.CurrentUserDto;
import edu.pjwstk.api.auth.dto.RotateUserTokensCommand;
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
