package edu.pjwstk.gamification.usecase.getrequiredexperiencebylevelid;

import edu.pjwstk.core.Command;
import jakarta.validation.ValidationException;

public record GetRequiredExperienceByLevelIdCommand(Integer levelId) implements Command {
    @Override
    public void validate() {
        if(levelId == null){
            throw new ValidationException("levelId cannot be null");
        }
    }
}
