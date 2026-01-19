package pl.gamilife.gamification.domain.service.impl;

import org.instancio.Instancio;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.context.ApplicationEventPublisher;
import pl.gamilife.gamification.domain.model.Item;
import pl.gamilife.gamification.domain.model.Level;
import pl.gamilife.gamification.domain.model.projection.GamificationUser;
import pl.gamilife.gamification.domain.port.context.UserContext;
import pl.gamilife.gamification.domain.port.repository.LevelRepository;
import pl.gamilife.gamification.domain.service.UserInventoryService;
import pl.gamilife.shared.kernel.event.LevelUpEvent;

import java.util.List;
import java.util.Optional;
import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;
import static org.instancio.Select.field;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class LevelServiceImplTest {

    @Mock
    private UserInventoryService userInventoryService;

    @Mock
    private LevelRepository levelRepository;

    @Mock
    private UserContext userContext;

    @Mock
    private ApplicationEventPublisher eventPublisher;

    @InjectMocks
    private LevelServiceImpl levelService;

    @Test
    void shouldLevelUpUser_whenGainedLevelsIsNotEmpty() {
        // given
        GamificationUser initialUser = Instancio.create(GamificationUser.class);
        Level level1 = Instancio.of(Level.class)
                .set(field(Level::getId), 2)
                .set(field(Level::getItems), Set.of(Instancio.create(Item.class)))
                .create();
        Level level2 = Instancio.of(Level.class)
                .set(field(Level::getId), 3)
                .set(field(Level::getItems), Set.of(Instancio.create(Item.class)))
                .create();

        List<Level> gainedLevels = List.of(level1, level2);
        GamificationUser updatedUser = new GamificationUser(initialUser.userId(), initialUser.username(), 3, 100, 50, 1L);

        when(userContext.levelUpUser(initialUser.userId(), 3)).thenReturn(updatedUser);

        // when
        GamificationUser result = levelService.levelUpUser(initialUser, gainedLevels);

        // then
        assertThat(result).isEqualTo(updatedUser);
        verify(userInventoryService).addItemsToUsersInventory(eq(initialUser.userId()), anySet());
        verify(userContext).levelUpUser(initialUser.userId(), 3);
        verify(eventPublisher).publishEvent(any(LevelUpEvent.class));
    }

    @Test
    void shouldReturnInitialUser_whenGainedLevelsIsEmpty() {
        // given
        GamificationUser initialUser = Instancio.create(GamificationUser.class);
        List<Level> gainedLevels = List.of();

        // when
        GamificationUser result = levelService.levelUpUser(initialUser, gainedLevels);

        // then
        assertThat(result).isEqualTo(initialUser);
        verifyNoInteractions(userInventoryService);
        verifyNoInteractions(userContext);
        verifyNoInteractions(eventPublisher);
    }

    @Test
    void shouldReturnLevels_whenUserIsEligible() {
        // given
        int currentLevel = 1;
        int experience = 1000;
        List<Level> expectedLevels = List.of(Instancio.create(Level.class));

        when(levelRepository.findLevelsGainedOrderByLevelAsc(currentLevel, experience)).thenReturn(expectedLevels);

        // when
        List<Level> result = levelService.checkIfUserEligibleForLevelUp(currentLevel, experience);

        // then
        assertThat(result).isEqualTo(expectedLevels);
    }

    @Test
    void shouldReturnEmptyList_whenUserIsNotEligible() {
        // given
        int currentLevel = 1;
        int experience = 10;

        when(levelRepository.findLevelsGainedOrderByLevelAsc(currentLevel, experience)).thenReturn(List.of());

        // when
        List<Level> result = levelService.checkIfUserEligibleForLevelUp(currentLevel, experience);

        // then
        assertThat(result).isEmpty();
    }

    @Test
    void shouldReturnEmptyListForEligibility_whenNegativeExperienceProvided() {
        // given
        when(levelRepository.findLevelsGainedOrderByLevelAsc(1, -100)).thenReturn(List.of());

        // when
        List<Level> result = levelService.checkIfUserEligibleForLevelUp(1, -100);

        // then
        assertThat(result).isEmpty();
    }

    @Test
    void shouldReturnNextLevel_whenItExists() {
        // given
        int currentLevel = 1;
        Level nextLevel = Instancio.of(Level.class)
                .set(field(Level::getId), 2)
                .create();

        when(levelRepository.findByLevel(2)).thenReturn(Optional.of(nextLevel));

        // when
        Optional<Level> result = levelService.getNextLevel(currentLevel);

        // then
        assertThat(result).isPresent().contains(nextLevel);
    }

    @Test
    void shouldReturnEmptyOptional_whenNextLevelDoesNotExist() {
        // given
        int currentLevel = 100;

        when(levelRepository.findByLevel(101)).thenReturn(Optional.empty());

        // when
        Optional<Level> result = levelService.getNextLevel(currentLevel);

        // then
        assertThat(result).isEmpty();
    }
}