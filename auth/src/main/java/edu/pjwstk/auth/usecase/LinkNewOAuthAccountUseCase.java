package edu.pjwstk.auth.usecase;

import edu.pjwstk.auth.dto.service.AuthTokens;
import edu.pjwstk.auth.dto.service.LinkOAuthAccountDto;

import java.util.Optional;

public interface LinkNewOAuthAccountUseCase {
    Optional<AuthTokens> execute(LinkOAuthAccountDto linkOAuthAccountDto);
}
