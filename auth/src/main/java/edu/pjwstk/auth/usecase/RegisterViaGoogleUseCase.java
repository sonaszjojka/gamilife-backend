package edu.pjwstk.auth.usecase;

import edu.pjwstk.auth.usecase.result.GoogleLoginResult;
import edu.pjwstk.auth.dto.service.GoogleUserDto;

@Deprecated
public interface RegisterViaGoogleUseCase {
    GoogleLoginResult execute(GoogleUserDto googleUserDto);
}
