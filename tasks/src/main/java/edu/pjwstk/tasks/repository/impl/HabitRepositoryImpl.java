package edu.pjwstk.tasks.repository.impl;

import edu.pjwstk.tasks.entity.Habit;
import edu.pjwstk.tasks.repository.HabitRepository;
import edu.pjwstk.tasks.repository.jpa.HabitRepositoryJpa;
import org.springframework.stereotype.Repository;

import java.util.Optional;
import java.util.UUID;

@Repository
public class HabitRepositoryImpl implements HabitRepository {

    private final HabitRepositoryJpa repositoryJpa;

    public HabitRepositoryImpl(HabitRepositoryJpa repositoryJpa) {
        this.repositoryJpa = repositoryJpa;
    }

    @Override
    public Habit save(Habit habit) {
        return repositoryJpa.save(habit);
    }

    @Override
    public Optional<Habit> findById(UUID habitId) {
        return repositoryJpa.findById(habitId);
    }

    @Override
    public void deleteById(UUID habitId) {
        repositoryJpa.deleteById(habitId);
    }

}
