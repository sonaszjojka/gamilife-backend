package edu.pjwstk.auth.usecase;

import edu.pjwstk.auth.dto.service.GoogleLoginDTO;
import edu.pjwstk.auth.dto.service.GoogleUserDto;

public interface RegisterViaGoogleUseCase {
    GoogleLoginDTO execute(GoogleUserDto googleUserDto);
}
