package edu.pjwstk.tasks.repository.jpa;


import edu.pjwstk.tasks.domain.Habit;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface HabitRepositoryJpa extends JpaRepository<Habit, UUID> {
}
