package edu.pjwstk.user.dto.service;

import pl.gamilife.infrastructure.core.architecture.Command;

public record GetUsersCommand(
        String username,
        int page,
        int size
) implements Command {
    @Override
    public void validate() {
        
    }
}
