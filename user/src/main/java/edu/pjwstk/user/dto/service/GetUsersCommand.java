package edu.pjwstk.user.dto.service;

import edu.pjwstk.core.Command;

public record GetUsersCommand(
        String username,
        int page,
        int size
) implements Command {
    @Override
    public void validate() {
        
    }
}
