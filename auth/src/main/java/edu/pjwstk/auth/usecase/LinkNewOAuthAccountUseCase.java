package edu.pjwstk.auth.usecase;

import edu.pjwstk.auth.dto.service.LinkOAuthAccountDto;
import edu.pjwstk.auth.dto.service.LoginUserResult;

import java.util.Optional;

public interface LinkNewOAuthAccountUseCase {
    Optional<LoginUserResult> execute(LinkOAuthAccountDto linkOAuthAccountDto);
}
