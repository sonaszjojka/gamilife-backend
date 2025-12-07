package pl.gamilife.gamification.application.usecase.getrequiredexperiencebylevelid;

import jakarta.validation.ValidationException;
import pl.gamilife.shared.kernel.architecture.Command;

public record GetRequiredExperienceByLevelIdCommand(Integer levelId) implements Command {
    @Override
    public void validate() {
        if(levelId == null){
            throw new ValidationException("levelId cannot be null");
        }
    }
}
