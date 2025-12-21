package pl.gamilife.auth.infrastructure.web.response;

import java.util.UUID;

public record OAuth2LinkResponse(
        String provider,
        String providerId,
        UUID userId
) {
}
