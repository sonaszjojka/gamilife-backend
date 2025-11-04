package edu.pjwstk.auth.usecase.refreshtoken;

import edu.pjwstk.core.Command;

public record RefreshAccessTokenCommand(String refreshToken) implements Command {
    @Override
    public void validate() {

    }
}
