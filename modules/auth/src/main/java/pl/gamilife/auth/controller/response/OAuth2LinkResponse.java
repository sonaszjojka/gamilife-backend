package pl.gamilife.auth.controller.response;

import java.util.UUID;

public record OAuth2LinkResponse(
        String provider,
        String providerId,
        UUID userId
) {
}
