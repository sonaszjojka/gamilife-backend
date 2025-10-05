package edu.pjwstk.tasks.repository.impl;

import edu.pjwstk.tasks.domain.Habit;
import edu.pjwstk.tasks.repository.HabitRepository;
import edu.pjwstk.tasks.repository.jpa.HabitRepositoryJpa;
import org.springframework.stereotype.Repository;

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

}
