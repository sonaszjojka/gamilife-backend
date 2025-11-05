package edu.pjwstk.auth.service;

import java.util.UUID;

public interface SecureCodesAndTokensService {

    void revokeAllTokensAndCodesForUser(UUID userId);

}
