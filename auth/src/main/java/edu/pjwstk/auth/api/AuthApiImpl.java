package edu.pjwstk.auth.api;

import edu.pjwstk.api.auth.AuthApi;
import edu.pjwstk.api.auth.dto.AuthTokens;
import edu.pjwstk.api.auth.dto.ChangePasswordDto;
import edu.pjwstk.api.auth.dto.CurrentUserDto;
import edu.pjwstk.api.auth.dto.RotateUserTokensDto;
import edu.pjwstk.auth.usecase.changepassword.ChangePasswordCommand;
import edu.pjwstk.auth.usecase.changepassword.ChangePasswordUseCase;
import edu.pjwstk.auth.usecase.getauthuser.GetAuthenticatedUserCommand;
import edu.pjwstk.auth.usecase.getauthuser.GetAuthenticatedUserDataUseCase;
import edu.pjwstk.auth.usecase.rotatetokens.RotateUserTokensCommand;
import edu.pjwstk.auth.usecase.rotatetokens.RotateUserTokensUseCase;
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
    public CurrentUserDto getCurrentUser() {
        return getAuthenticatedUserDataUseCase.execute(new GetAuthenticatedUserCommand());
    }

    @Override
    public String handleChangePassword(ChangePasswordDto dto) {
        return changePasswordUseCase.execute(ChangePasswordCommand.from(dto));
    }

    @Override
    public AuthTokens rotateUserTokens(RotateUserTokensDto dto) {
        return rotateUserTokensUseCase.execute(RotateUserTokensCommand.from(dto));
    }

}
