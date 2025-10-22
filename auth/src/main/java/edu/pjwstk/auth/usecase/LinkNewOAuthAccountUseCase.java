package edu.pjwstk.auth.usecase;

import edu.pjwstk.auth.dto.service.LinkOAuthAccountDto;
import edu.pjwstk.common.authApi.dto.AuthTokens;

import java.util.Optional;

public interface LinkNewOAuthAccountUseCase {
    Optional<AuthTokens> execute(LinkOAuthAccountDto linkOAuthAccountDto);
}
