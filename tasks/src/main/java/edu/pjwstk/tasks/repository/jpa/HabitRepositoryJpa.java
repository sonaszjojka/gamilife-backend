package edu.pjwstk.tasks.repository.jpa;


import edu.pjwstk.tasks.entity.Habit;
import edu.pjwstk.tasks.entity.Task;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.UUID;

public interface HabitRepositoryJpa extends JpaRepository<Habit, UUID> {
   Habit findHabitById(UUID id);
}
