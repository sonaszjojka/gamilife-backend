package pl.gamilife.gamification.domain.model;

import org.instancio.Instancio;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;
import static org.instancio.Select.field;

class LevelTest {

    @Test
    void shouldReturnCorrectLevel_whenIdIsProvided() {
        // given
        Integer expectedLevel = 5;
        Level level = Instancio.of(Level.class)
                .set(field(Level::getId), 5)
                .create();

        // when
        Integer result = level.getLevel();

        // then
        assertThat(result).isEqualTo(expectedLevel);
    }

    @Test
    void shouldReturnNullLevel_whenIdIsNull() {
        // given
        Level level = Instancio.of(Level.class)
                .set(field(Level::getId), null)
                .create();

        // when
        Integer result = level.getLevel();

        // then
        assertThat(result).isNull();
    }
}
