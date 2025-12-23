package pl.gamilife.app.persistence.jpa;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import pl.gamilife.app.persistence.view.ActivityItemWithPomodoro;

import java.util.UUID;

public interface JpaActivityItemWithPomodoroRepository extends JpaRepository<ActivityItemWithPomodoro, UUID>, JpaSpecificationExecutor<ActivityItemWithPomodoro> {
}