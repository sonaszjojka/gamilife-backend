package pl.gamilife.auth.application.port;

import java.util.Map;

public interface GoogleAuthClient {
    Map<String, String> call(String code, String codeVerifier);
}
