package pl.gamilife.app.view;

import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface JpaActivityItemWithPomodoroRepository extends JpaRepository<ActivityItemWithPomodoro, UUID> {
}