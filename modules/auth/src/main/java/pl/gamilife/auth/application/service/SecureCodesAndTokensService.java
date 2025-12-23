package pl.gamilife.auth.application.service;

import java.util.UUID;

public interface SecureCodesAndTokensService {

    void revokeAllTokensAndCodesForUser(UUID userId);

}
