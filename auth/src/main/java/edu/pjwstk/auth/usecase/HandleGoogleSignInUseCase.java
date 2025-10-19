package edu.pjwstk.auth.usecase;

import edu.pjwstk.auth.dto.service.GoogleLoginDTO;
import edu.pjwstk.auth.dto.service.OAuthCodeDto;

public interface HandleGoogleSignInUseCase {
    GoogleLoginDTO execute(OAuthCodeDto oAuthCodeDto);
}
